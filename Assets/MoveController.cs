using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MoveController : MonoBehaviour
{
    private Rigidbody rb;
    public float movementSpeed = 2f, sprintModifier = 1.5f, aimModifier = 0.75f, speedModifier;
    Vector3 movementVector;
    private Transform cam;

    public AudioSource sounds;
    public List<AudioClip> steps = new List<AudioClip>();
    float t = 0;

    // Start is called before the first frame update
    void Start()
    {
        rb = GetComponent<Rigidbody>();
        cam = transform.GetChild(0).GetChild(0);
    }

    // Update is called once per frame
    void Update()
    {
        speedModifier = Input.GetMouseButton(1) ? aimModifier : (Input.GetButton("Sprint") ? sprintModifier : 1);
        movementVector = new Vector3(Input.GetAxis("Horizontal"), 0, Input.GetAxis("Vertical"));
        TranslateMovement(CameraForward(movementVector));
        if (movementVector != Vector3.zero)
        {
            Footsteps();
        }
    }

    Vector3 CameraForward(Vector3 input)
    {
        Vector3 output = Vector3.zero;
        output += cam.forward * input.z * movementSpeed * speedModifier;
        output += cam.right * input.x * movementSpeed * 0.75f;
        return output;
    }

    public void TranslateMovement(Vector3 input)
    {
        rb.velocity = new Vector3(input.x, rb.velocity.y, input.z);
    }

    public void Footsteps()
    {
        if (t == 0)
        {
            sounds.PlayOneShot(steps[Random.Range(0, 6)]);
        }
        t += Time.deltaTime;
        float stepDelay = (2 - speedModifier)/2;
        if (t >= stepDelay)
        {
            t = 0;
        }
    }
}
