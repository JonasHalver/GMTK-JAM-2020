﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Shooting : MonoBehaviour
{
    Camera cam;
    public float weaponDamage = 1f;
    public Light muzzleFlash;
    public LayerMask targetMask, obstacleMask;
    public GameObject trail;

    AudioSource sound;

    // Start is called before the first frame update
    void Start()
    {
        cam = Camera.main;
        sound = GetComponent<AudioSource>();
    }

    // Update is called once per frame
    void Update()
    {
        if (Input.GetButtonDown("Fire1") && !Input.GetButton("Sprint"))
        {
            Shoot();
        }
    }

    public void Shoot()
    {
        Life target = null;

        sound.Play();

        RaycastHit hit;
        Vector2 midpoint = new Vector2(Screen.width / 2, Screen.height / 2);
        Ray ray = cam.ScreenPointToRay(midpoint);

        if (!Physics.Raycast(ray, out hit, Mathf.Infinity, obstacleMask))
        {
            if (Physics.Raycast(ray, out hit, Mathf.Infinity, targetMask))
            {
                if (hit.collider.GetComponent<Life>())
                {
                    target = hit.collider.GetComponent<Life>();
                }
            }
        }

        if (hit.collider)
        {
            StartCoroutine(Flash(hit.point));
        }

        if (target)
        {
            target.Hit(target.isHead ? weaponDamage * 2 : weaponDamage);
        }

    }

    IEnumerator Flash(Vector3 hitPoint)
    {
        muzzleFlash.enabled = true;
        GameObject newTrail = Instantiate(trail, transform.position, cam.transform.rotation);
        newTrail.transform.LookAt(hitPoint);
        newTrail.GetComponent<ParticleSystem>().Play();
        yield return new WaitForSeconds(0.1f);
        muzzleFlash.enabled = false;

    }
}
